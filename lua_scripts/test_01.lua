--[[ 
	 Test script for lua engine
--]]


-- Doubles value of m. If m = nil return 1 
function computeX(m) 
	if m == nil then
		return 1
	else 
		return m * 2
	end
end

-- Send hello message
function send_hello_msg(socket) 
	msg = "Hello"
	socket:send(msg)
end

-- Handles events 
function on_event(event_name, socket) 
	if event_name == "connect" then 
		send_hello_msg(socket)
	end
	if event_name == "disconnect" then
		socket:reset()
	end
end		


-- Mock socket 	
Socket = { name = "MockSocket" }

function Socket:new(id)
	self.id = id
	self.queue = {}
	return self;
end
	
function Socket:send(msg) 
	self.queue[1] = msg;
end	

function Socket:reset() 
	self.queue = {}
end

	

	
-- Simple self test. Use lua to run it	
--[[
print("Compute X m = 2: ", computeX(2))
print("Compute X m = ?: ", computeX())	

s = Socket:new(123)
print("Socket id: ", s.id)
print("Socket name: ", s.name)

send_hello_msg(s)
print("Socket queue: ", s.queue[1])
on_event("disconnect", s)
print("Socket queue: ", s.queue[1])
--]]


