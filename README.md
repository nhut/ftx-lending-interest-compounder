# ftx-lending-interest-compounder

Repeat lending exists assets in FTX with earned interests back to lending pool every hour. In short, compound lending interest back to the lending.

Requirements:
* Java 17 LTS

## How to run
For Windows:
1. Copy run.cmd to myrun.cmd.
2. Edit myrun.cmd-file and replace "app.ftx.api.key"- and "app.ftx.api.secret"-key's values.
3. Run myrun.cmd
Press CTRL + C to stop the application or close it.

For Linux:
1. Open terminal.
2. Copy run.sh to myrun.sh
3. Give execution permission "chmod +x myrun.sh"
4. Edit myrun.sh-file and replace "app.ftx.api.key"- and "app.ftx.api.secret"-key's values.
5. Run myrun.sh
Press CTRL + C to stop the application or close it.
