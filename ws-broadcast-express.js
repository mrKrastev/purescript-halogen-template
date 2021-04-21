'use strict';

const express = require('express');
const WebSocket = require('ws');

const PORT = process.env.PORT || 3000;
var express = require('express');
var app = express();


app.use('/', express.static('/dist'), serveIndex('/dist'));
/*const server = express()
.get('/', (req,res) => res.sendFile(__dirname + '/dev/index.html'))
.get('/', (req,res) => res.sendFile(__dirname + '/dev/app.js'))
.get('/', (req,res) => res.sendFile(__dirname + '/dev/app2.js'))


.get('/index.js', (req,res) => res.sendFile(__dirname + '/index.js'))*/
app.listen(PORT, () => console.log(`Express listening on ${PORT}`));

const wss = new WebSocket.Server({ server });

wss.on('connection', function connection(ws) {
  ws.on('message', function incoming(data) {
    // console.log("Broadcasting: " + data)
    wss.clients.forEach(function each(client) {
      if (client !== ws && client.readyState === WebSocket.OPEN) {
      // if (client.readyState === WebSocket.OPEN) {
          client.send(data);
      }
    });
  });
});

console.log("WebSocket broadcast server configured on " + wss);