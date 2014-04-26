[ -n "$PS1" ] && source ~/.bash_profile

export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk1.7.0_51.jdk/Contents/Home"
export PP_HOME=~/Development/workspace/billpay
export JBOSS_HOME=~/Development/bin/jboss-as-7.1.1.Final
export JAVA_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,address=8787,server=y,suspend=n -Dpp.home=$PP_HOME -server -Xms64m -Xmx512m -XX:MaxPermSize=512m -Djava.net.preferIPv4Stack=true -Dorg.jboss.resolver.warning=true -Dsun.rmi.dgc.client.gcInterval=3600000 -Dsun.rmi.dgc.server.gcInterval=3600000 -Djboss.modules.system.pkgs=org.jboss.byteman -Djava.awt.headless=true"
export M2_HOME=~/Development/bin/apache-maven-3.0.5
export M2=$M2_HOME/bin
export MAVEN_OPTS="-Xms256m -Xmx512m"
export EC2_HOME=~/Development/bin/ec2-api-tools-1.6.8.0
export PATH=$M2:~/Development/bin/sbt/bin:/usr/local/share/npm/bin:/usr/local/bin:/usr/local/heroku/bin:$PATH
. ~/.bash_prompt
export TERM="xterm-16color"
source "$HOME/.homesick/repos/homeshick/homeshick.sh"
source "$HOME/.homesick/repos/homeshick/completions/homeshick-completion.bash"
. `brew --prefix`/etc/profile.d/z.sh
