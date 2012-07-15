/**
 * test_01.js 
 * Functions: 
 *  computeX(m) - doubles value of m 
 *  send_hello_msg(socket) - sends message to mock socket
 *	on_event(event_name, data, socket) - handles events 'connect' and 'disconnect'
 */


// Doubles value of m. If m undefined - return 1
function computeX(m) { 
	return typeof m == 'undefined' ? 1 : m * 2;
}

// Sends message to mock socket 
function send_hello_msg(socket) { 
	var msg = { 
		type: "chat.msg",
		data: "Hello from socket " + socket.id
	};
	socket.send(msg);
}


// handles events
function on_event(event_name, socket) { 
	if (event_name == 'connect') { 
		var event = { 
			type: "connect"
		};
		socket.send(event);
	} else if (event_name == 'disconnect') { 
		socket.reset();	
	} else { 
		throw new Error("Unknown event: " + event_name);
	} 	
}


var Socket = function Socket(id) { 
	this.id = id;
	this.queue = [];
	
	return this;
};

Socket.prototype = { 

	send: function(msg) { 
		this.queue.push(msg);		
	},

	receive: function() { 
		return this.queue.pop();
	},
	
	reset: function() { 
		this.queue = [];
	}
};


// Simple self test. Use node.js 
/*
var socket = new Socket(123);

console.log("Compute X m = undefined: ", computeX());
console.log("Compute X m = 2: ", computeX(2));

send_hello_msg(socket);
console.log("Messages in queue: ", socket.queue);

var next_msg = socket.receive();
if (typeof next_msg != 'undefined') { 
	on_event('connect', socket);
	console.log("Messages in queue: ", socket.queue);
	on_event('disconnect', socket);
	console.log("Messages in queue: ", socket.queue);
}
*/






