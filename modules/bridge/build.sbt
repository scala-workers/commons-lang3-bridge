import ProjectKeys._

libraryDependencies += Dependents.commonsLang3 % Provided
libraryDependencies += Dependents.scalactic
libraryDependencies += Dependents.scalatest % Test

CommonSettings.commonProjectSettings
