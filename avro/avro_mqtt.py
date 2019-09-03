import paho.mqtt.client as mqtt
import io
import json
import avro.schema
import avro.datafile
import avro.io
import avro.ipc
import time

# Got this schema from https://avro.apache.org/docs/current/gettingstartedjava.html
SCHEMA = avro.schema.Parse(json.dumps({
 "name"         : "User",
 "type"         : "record",
 "fields"       : [
     {"name": "name"            , "type": "string"},
     {"name": "favorite_number" , "type": ["int", "null"]},
     {"name": "favorite_color"  , "type": ["string", "null"]}
 ]
}))

# The callback for when the client receives a CONNACK response from the server.
def on_connect(client, obj, flags, rc):
    print("Connected with result code "+str(rc))
    # Subscribing in on_connect() means that if we lose the connection and
    # reconnect then subscriptions will be renewed.
    client.subscribe("t/#")
    time.sleep(0.5)
    publish_msg(client)
    client.unsubscribe("t/#")

def publish_msg(client):
    datum_w = avro.io.DatumWriter(SCHEMA)
    buf = io.BytesIO()
    encoder = avro.io.BinaryEncoder(buf)
    datum_w.write({"name": "Shawn", "favorite_number": 666, "favorite_color": "red"}, encoder)
    message = buf.getvalue()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)

# The callback for when a PUBLISH message is received from the server.
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))

client = mqtt.Client(client_id = "111")
client.reconnect_delay_set(min_delay=120, max_delay=121)
client.on_connect = on_connect
client.on_message = on_message

client.connect("127.0.0.1", 1883, 300)

# Blocking call that processes network traffic, dispatches callbacks and
# handles reconnecting.
# Other loop*() functions are available that give a threaded interface and a
# manual interface.
client.loop_forever()
