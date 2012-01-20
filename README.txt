HOW TO SETUP DEVELOPMENT ENVIRONMENT

1. Open project in IDEA

2. Add missing SDKs:
   File > Project Structure > Platform Settings > SDKs
   Add new JSDK with name "IDEA JDK"
   Add tools.jar from JDK_HOME/lib to "IDEA JDK" classpath
   Add new Intellij IDEA plugin SDK with name "IDEA SDK"
   Double check that you have named both SDKs correctly

3. [Optional] Remove unused plugins to minimize IDEA sturtup time:
   Run plugin using "ceylon-idea-plugin" configuration. New instance of IDEA will start.
   File > Settings > IDE Settings > Plugins
   Uncheck all plugins except "ceylon-idea-plugin"

4. [Optional] Install "PsiViewer" plugin
   
See also: http://confluence.jetbrains.net/display/IDEADEV/Getting+Started+with+Plugin+Development
