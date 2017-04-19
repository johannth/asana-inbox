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

const createClientsFromRequest = req => {
  const tokens = req.headers.authorization.replace('Bearer ', '').split(',');
  return createClients(tokens);
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

const taskFromRaw = rawTask => {
  return {
    id: `${rawTask.id}`,
    dueOn: rawTask.due_on,
    name: rawTask.name,
    projects: rawTask.projects.map(projectFromRaw),
    workspace: workspaceFromRaw(rawTask.workspace),
    assigneeStatus: rawTask.assignee_status,
  };
};

const projectFromRaw = rawProject => {
  return {
    id: `${rawProject.id}`,
    name: rawProject.name,
  };
};

const workspaceFromRaw = rawWorkspace => {
  return {
    id: `${rawWorkspace.id}`,
    name: rawWorkspace.name,
  };
};

app.get('/api/tasks', (req, res) => {
  const clients = createClientsFromRequest(req);
  Promise.all(clients.map(client => allTasks(client)))
    .then(tasksPerWorkspace =>
      tasksPerWorkspace.reduce((acc, next) => acc.concat(next), []),
    )
    .then(tasks => tasks.map(taskFromRaw))
    .then(tasks => res.json({tasks}))
    .error(error => {
      res.status(500);
      res.json({error});
    });
});

const mutationsToRaw = body => {
  return {
    due_on: body.dueOn,
    name: body.name,
    assignee_status: body.assigneeStatus,
    completed: body.completed,
  };
};

app.post('/api/tasks/:workspaceId/:taskId', (req, res) => {
  const clients = createClientsFromRequest(req);
  Promise.all(
    clients.map(client => {
      return client.users.me().then(me => {
        const workspace = me.workspaces.find(workspace => {
          return workspace.id == req.params.workspaceId;
        });

        if (workspace) {
          return client.tasks.update(
            req.params.taskId,
            mutationsToRaw(req.body),
          );
        } else {
          return Promise.resolve();
        }
      });
    }),
  )
    .then(response => res.json({}))
    .error(error => res.json({error}));
});

// process.env.PORT lets the port be set by Heroku
const port = process.env.PORT || 8080;

app.listen(port, () => {
  console.log(`App listening on port ${port}!`);
});
