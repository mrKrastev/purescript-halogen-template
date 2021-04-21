'use strict';

const express = require('express');
const WebSocket = require('ws');

const PORT = process.env.PORT || 3000;

const server = express();
server.use('dist');
server.listen(PORT, () => console.log(`Express listening on ${PORT}`));
/*.get('/', (req,res) => res.sendFile(__dirname + '/dev/index.html'))
.get('/index.js', (req,res) => res.sendFile(__dirname + '/index.js'))*/
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