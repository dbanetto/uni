import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.File;
import java.util.*;
import java.util.List;

@SuppressWarnings("serial")
public class GUI extends JFrame {

    private JTextField ipText;
    private JTextField nameText;
    private JButton actionButton;
    private JButton addButton;

    private DNSDB dnsDB;

    public GUI() {
        super("DNS DB with B+ Tree");
        setDefaultCloseOperation(EXIT_ON_CLOSE);

        dnsDB = new DNSDB();

        setLayout(new BorderLayout());
        ipText = new JTextField();
        ipText.setColumns(15);
        ipText.addKeyListener(new KeyAdapter() {

            @Override
            public void keyReleased(KeyEvent e) {
                validateButton();
            }
        });
        nameText = new JTextField();
        nameText.setColumns(15);
        nameText.addKeyListener(new KeyAdapter() {

            @Override
            public void keyPressed(KeyEvent e) {
                validateButton();
            }
        });

        JPanel labels = new JPanel();
        labels.setLayout(new BoxLayout(labels, BoxLayout.Y_AXIS));
        labels.add(new JLabel("IP: "));
        labels.add(Box.createVerticalStrut(10));
        labels.add(new JLabel("Name: "));
        labels.setBorder(new EmptyBorder(5, 5, 5, 5));

        JPanel fields = new JPanel();
        fields.setLayout(new BoxLayout(fields, BoxLayout.Y_AXIS));
        fields.add(ipText);
        fields.add(Box.createVerticalStrut(5));
        fields.add(nameText);
        fields.setBorder(new EmptyBorder(5, 5, 10, 5));

        add(labels, BorderLayout.WEST);
        add(fields, BorderLayout.CENTER);

        actionButton = new JButton();
        actionButton.setMaximumSize(new Dimension(250, 50));
        actionButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                handleAction();
            }
        });

        addButton = new JButton();
        addButton.setMaximumSize(new Dimension(250, 50));
        addButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                handleAdd();
            }
        });

        validateButton();

        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
        buttonPanel.add(actionButton);
        buttonPanel.add(Box.createVerticalStrut(5));
        buttonPanel.add(addButton);
        add(buttonPanel, BorderLayout.SOUTH);


        JButton iterateAll = new JButton("Iterate All Pairs");
        iterateAll.setMaximumSize(new Dimension(250, 50));
        iterateAll.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                dnsDB.iterateAll();
            }
        });

        JButton runTest = new JButton("Run Test");
        runTest.setMaximumSize(new Dimension(250, 50));
        runTest.addActionListener(new ActionListener() {

            @Override
            public void actionPerformed(ActionEvent e) {
                JFileChooser chooser = new JFileChooser(".");//System.getProperty("user.dir"));
                int res = chooser.showOpenDialog(GUI.this);
                if (res == JFileChooser.APPROVE_OPTION) {
                    dnsDB.testAllPairs(chooser.getSelectedFile());
                }
            }
        });

        buttonPanel.add(Box.createVerticalStrut(5));
        buttonPanel.add(iterateAll);
        buttonPanel.add(Box.createVerticalStrut(5));
        buttonPanel.add(runTest);

        setLocationRelativeTo(null);
        pack();
        setVisible(true);
    }

    public static void main(String[] args) {
        File file = new File("host-list.txt");
        if (args.length > 0) {
            File f = new File(args[0]);
            if (f.exists()) file = f;
        }
        new GUI().load(file);
    }

    private void validateButton() {
        String ip = ipText.getText().trim();
        String name = nameText.getText().trim();

        if (ip.isEmpty() && name.isEmpty()) {
            actionButton.setText("No Input");
            actionButton.setEnabled(false);
            addButton.setText("Can't Add");
            addButton.setEnabled(false);
        } else if (ip.isEmpty()) {
            actionButton.setText("Find IP");
            actionButton.setEnabled(true);
            addButton.setText("Can't Add");
            addButton.setEnabled(false);
        } else if (name.isEmpty()) {
            actionButton.setText("Find Host Name");
            actionButton.setEnabled(true);
            addButton.setText("Can't Add");
            addButton.setEnabled(false);
        } else {
            actionButton.setText("Test Name-IP Pair");
            actionButton.setEnabled(true);
            addButton.setText("Add Name-IP pair");
            addButton.setEnabled(true);
        }
    }

    private void handleAction() {
        if (actionButton.getText().equals("Find IP")) {
            List<Integer> ip = dnsDB.findIP(nameText.getText());
            if (ip.isEmpty()) {
                JOptionPane.showMessageDialog(GUI.this, "Could not find an IP address with host name: " + nameText.getText());
            } else if (ip.size() == 1) {
                ipText.setText(DNSDB.IPToString(ip.get(0)));
            } else {
                StringBuilder sb = new StringBuilder();
                for (Integer i : ip) {
                    if (i != ip.get(0)) {
                        sb.append(", ");
                    }
                    sb.append(DNSDB.IPToString(i));

                }
                ipText.setText(sb.toString());
            }
        } else if (actionButton.getText().equals("Find Host Name")) {
            Integer ip = DNSDB.stringToIP(ipText.getText());
            if (ip == null) {
                JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is not a valid IP address.");
            } else {
                List<String> name = dnsDB.findHostName(ip);
                if (name.isEmpty()) {
                    JOptionPane.showMessageDialog(GUI.this, "Could not find a host name with IP address: " + ipText.getText());
                } else if (name.size() == 0) {
                    nameText.setText(name.get(0));
                } else {
                    StringBuilder sb = new StringBuilder();
                    for (String n : name) {
                        if (n != name.get(0)) {
                            sb.append(", ");
                        }
                        sb.append(n);

                    }
                    nameText.setText(name.get(0));
                }
            }
        } else if (actionButton.getText().equals("Test Name-IP Pair")) {
            Integer ip = DNSDB.stringToIP(ipText.getText());
            if (ip == null) {
                JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is not a valid IP address.");
            } else {
                boolean valid = dnsDB.testPair(ip, nameText.getText());
                if (valid) {
                    JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is correctly mapped to '" + nameText.getText() + "'.");
                } else {
                    JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is not the IP address for '" + nameText.getText() + "'.");
                }
            }
        }
    }

    private void handleAdd() {
        Integer ip = DNSDB.stringToIP(ipText.getText());
        if (ip == null) {
            JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is not a valid IP address.");
        } else {
            if (dnsDB.add(nameText.getText(), ip)) {
                JOptionPane.showMessageDialog(GUI.this, "'" + ipText.getText() + "' is now mapped to '" + nameText.getText() + "'.");
            } else {
                JOptionPane.showMessageDialog(GUI.this, "adding '" + ipText.getText() + "' - '" + nameText.getText() + "' failed.");
            }
        }
    }

    public void load(File f) {
        dnsDB.load(f);
    }

}
