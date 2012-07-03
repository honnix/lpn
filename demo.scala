import se.sics.prologbeans._

val session = new PrologSession
session.connect
val bindings = (new Bindings).bind("E", "good_to_sleep.")
val answer = session.executeQuery("evaluate(E, R)", bindings)
val result = answer.getValue("R")
if (result ne null) println(result) else println("error")
session.disconnect
