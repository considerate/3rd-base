import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import java.util.Scanner;

public class MqttPublishSample {

    public static void main(String[] args) {

        String topic        = "test";
        String content      = "Message from MqttPublishSample";
        int qos             = 2;
        String broker       = "tcp://10.0.1.116:1883";
        String clientId     = "JavaSample";
        MemoryPersistence persistence = new MemoryPersistence();



        try {
            MqttClient sampleClient = new MqttClient(broker, clientId, persistence);
            MqttConnectOptions connOpts = new MqttConnectOptions();
            connOpts.setCleanSession(true);
            System.out.println("Connecting to broker: "+broker);
            sampleClient.connect(connOpts);
            System.out.println("Connected");
            

            sampleClient.subscribe(topic);
            sampleClient.setCallback(new MqttCallback() {
                public void connectionLost(Throwable e) {}
                public void deliveryComplete(IMqttDeliveryToken token) {}
                public void messageArrived(String topic, MqttMessage message) {
                    try {
                    System.out.println(new String(message.getPayload(), "UTF-8"));
                } catch (Exception e) {}
                }
            });
            Scanner scan = new Scanner(System.in);
            boolean notdone = true;
            System.out.println("Chatroom is now open");
            while(scan.hasNext()){
                
                String text = scan.nextLine();
                MqttMessage message = new MqttMessage(text.getBytes());
                message.setQos(qos);
                sampleClient.publish(topic, message);
                
            }
            
            sampleClient.disconnect();
            System.out.println("Disconnected");
            System.exit(0);
        } catch(MqttException me) {
            System.out.println("reason "+me.getReasonCode());
            System.out.println("msg "+me.getMessage());
            System.out.println("loc "+me.getLocalizedMessage());
            System.out.println("cause "+me.getCause());
            System.out.println("excep "+me);
            me.printStackTrace();
        }
    }
}