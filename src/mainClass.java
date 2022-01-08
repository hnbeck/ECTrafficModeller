import java.util.List;

import org.jivesoftware.smack.chat2.Chat;
import org.jivesoftware.smack.chat2.ChatManager;
import org.jivesoftware.smack.chat2.IncomingChatMessageListener;
import org.jivesoftware.smack.packet.Message;
import org.jxmpp.jid.EntityBareJid;
import org.jxmpp.jid.impl.JidCreate;
import processing.core.PApplet;
import org.jivesoftware.smack.AbstractXMPPConnection;
import org.jivesoftware.smack.ConnectionConfiguration;
import org.jivesoftware.smack.tcp.XMPPTCPConnection;
import org.jivesoftware.smack.tcp.XMPPTCPConnectionConfiguration;

public class mainClass extends PApplet
{
    public static void main(String [] args)
    {
        PApplet.main(new String[] {mainClass.class.getName()});
    }
    @Override
    public void settings() {
        size(800, 360, "processing.opengl.PGraphics3D");
        try {
            XMPPTCPConnectionConfiguration config = XMPPTCPConnectionConfiguration.builder()
                    .setUsernameAndPassword("chef","1912che")
                    .setXmppDomain("localhost")
                    .setHost("localhost")
                    .setSecurityMode(ConnectionConfiguration.SecurityMode.disabled)
                    .build();

            AbstractXMPPConnection connection = new XMPPTCPConnection(config);
// Connect to the server
            connection.connect();
            connection.login();

            ChatManager chatManager = ChatManager.getInstanceFor(connection);
            chatManager.addIncomingListener(new IncomingChatMessageListener() {
                @Override
                public void newIncomingMessage(EntityBareJid from, Message message, Chat chat) {
                    System.out.println("New message from " + from + ": " + message.getBody());
                }
            });
            Chat chat = chatManager.chatWith(JidCreate.from("angestellter@localhost").asEntityBareJidOrThrow());

            chat.send("Halloooo again!");
            //connection.disconnect();
        }
        catch (Exception e)
        {
            System.out.println("Fehler  " + e);
        }

    }

    @Override
    public void draw() {
        background(0);
        camera(mouseX, height/2, (height/2) / tan(PI/6), width/2, height/2, 0, 0, 1, 0);
        translate(width/2, height/2, -100);
        stroke(255);
        noFill();
        box(200);
    }
}
