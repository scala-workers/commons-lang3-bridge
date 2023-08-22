import djx.sbt.depts.plugins.{PluginsCollection => projectPlugins}

addSbtPlugin(projectPlugins.`sbt-scalafmt`)
addSbtPlugin(projectPlugins.`sbt-twirl`)
addSbtPlugin(projectPlugins.`sbt-sonatype`)
addSbtPlugin(projectPlugins.`sbt-pgp`)

addDependencyTreePlugin
