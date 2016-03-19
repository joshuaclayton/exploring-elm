import StartApp
import Task
import Effects exposing (Never)

import Athena.View exposing (view)
import Athena.BaseModel exposing (initialModel, update)
import Athena.ApiClient exposing (getTopics)

app = StartApp.start { init = (initialModel, getTopics), view = view, update = update, inputs = [] }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks
