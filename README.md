# discord-vc-notification

__Notice: due to the limitation of machine resource, hosting the application yourselves is recommended at the moment.__

First, register an application on [Discord Developer Portal](https://discord.com/developers/applications) and obtain a bot token.

Install docker or Docker Desktop (on Windows/Mac), then run 

```
docker run DISCORD_BOT_TOKEN=<your bot token> docker.io/fumieval/discord-vc-notification:0.4.2
```

To enable notifications, add a line to the topic of the text channel as follows:

```
vc-notification: [NAME OF THE VOICE CHANNEL]
```

Specify a space-delimited list of channel names to monitor.

## Example

```
vc-notification: General
```

When someone joins the Voice Channel _General_, the bot posts a message to the
text channel.

You can add "first" keyword like `vc-notification: General first` to suppress notifications when anyone is in the channel already.
