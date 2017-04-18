import newrelic from 'newrelic';
import express from 'express';
import bodyParser from 'body-parser';
import cors from 'cors';
import asana from 'asana';
import Promise from 'bluebird';

const app = express();

app.use(bodyParser.json());
app.use(cors());

const handleErrors = response => {
  if (!response.ok) {
    throw Error(response.statusText);
  }
  return response;
};

const createClients = accessTokens => {
  return accessTokens.map(accessToken =>
    asana.Client.create().useAccessToken(accessToken),
  );
};

const allTasks = client => {
  return client.users
    .me()
    .then(me => {
      return Promise.all(
        me.workspaces.map(workspace => {
          return client.tasks.findAll({
            assignee: 'me',
            workspace: workspace.id,
            completed_since: 'now',
            limit: 100,
            opt_fields: 'id,name,assignee_status,due_on,projects,workspace,projects.name,workspace.name',
          });
        }),
      );
    })
    .then(tasksPerWorkspace => {
      return tasksPerWorkspace.reduce((acc, next) => acc.concat(next.data), []);
    });
};

const cleanTaskData = rawTask => {
  return {
    id: `${rawTask.id}`,
    dueOn: rawTask.due_on,
    name: rawTask.name,
    projects: rawTask.projects.map(cleanProjectData),
    workspace: cleanWorkspaceData(rawTask.workspace),
    assigneeStatus: rawTask.assignee_status,
  };
};

const cleanProjectData = rawProject => {
  return {
    id: `${rawProject.id}`,
    name: rawProject.name,
  };
};

const cleanWorkspaceData = rawWorkspace => {
  return {
    id: `${rawWorkspace.id}`,
    name: rawWorkspace.name,
  };
};

app.get('/api/tasks', (req, res) => {
  const tokens = req.headers.authorization.replace('Bearer ', '').split(',');
  const clients = createClients(tokens);
  Promise.all(clients.map(client => allTasks(client)))
    .then(tasksPerWorkspace =>
      tasksPerWorkspace.reduce((acc, next) => acc.concat(next), []),
    )
    .then(tasks => tasks.map(cleanTaskData))
    .then(tasks => res.send(JSON.stringify({tasks})))
    .error(error => {
      res.status(500);
      res.send(JSON.stringify({error}));
    });
});

// process.env.PORT lets the port be set by Heroku
const port = process.env.PORT || 8080;

app.listen(port, () => {
  console.log(`App listening on port ${port}!`);
});
