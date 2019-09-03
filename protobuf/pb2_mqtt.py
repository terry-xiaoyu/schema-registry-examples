import paho.mqtt.client as mqtt
import io
import addressbook_pb2
import time

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
    address_book = addressbook_pb2.AddressBook()
    p = address_book.people.add()
    p.id = 1
    p.name = "Shawn"
    p.email = "liuxy@emqx.io"
    phone_number = p.phones.add()
    phone_number.number = "18212120987"
    phone_number.type = addressbook_pb2.Person.MOBILE
    message = address_book.SerializeToString()
    topic = "t/1"
    print("publish to topic: t/1, payload:", message)
    client.publish(topic, payload=message, qos=0, retain=False)

# The callback for when a PUBLISH message is received from the server.
def on_message(client, userdata, msg):
    print(msg.topic+" "+str(msg.payload))

client = mqtt.Client(client_id = "protobuf")
client.reconnect_delay_set(min_delay=120, max_delay=121)
client.on_connect = on_connect
client.on_message = on_message

client.connect("127.0.0.1", 1883, 300)

# Blocking call that processes network traffic, dispatches callbacks and
# handles reconnecting.
# Other loop*() functions are available that give a threaded interface and a
# manual interface.
client.loop_forever()
