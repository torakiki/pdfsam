/*
Copyright (c) 2011 Dr. David Michel

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this software except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

For more information about this project, visit
   http://jhyperlink.sourceforge.net/

or contact us via email:
   dav_m@users.sourceforge.net

 */
package org.swingplus;

import java.awt.Color;
import java.awt.Cursor;
import java.awt.Desktop;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import javax.swing.JLabel;
import javax.swing.JOptionPane;

/**
 * A {@link JLabel} behaving like an HTML hyperlink usually found on web applications. By default, the style of the link is mimicking that found by default in web browsers:
 * <ul>
 * z
 * <li>When the user clicks on the link, the default web browser is launched and will try to connect to the web page defined by the link</li>
 * <li>When the hovering the mouse over the link, the cursor changes into a hand motif and the text is underlined.</li>
 * <li>The link will appear blue before it has been clicked for the first time, purple thereafter.</li>
 * </ul>
 * <p>
 * All this behaviour can be of course changes using the various public methods available.
 * <p>
 * Example code: <code>
 *    import org.swingplus.JHyperlink;
 *    ...
 *    JHyperlink link = new JHyperlink("website","http://www.mywebsite.com");
 * </code>
 */
public class JHyperlink extends JLabel {

    private String text;
    private URI uri;

    private boolean mouseEntered = false;
    private boolean underlinedWhenHovered = true;
    private boolean handCursorWhenHovered = true;
    private Color colorBeforeClick = Color.blue;
    private Color colorAfterClick = new Color(102, 14, 122); // purple colour

    /**
     * Constructor.
     * 
     * @param text
     *            text/label for the link
     * @param uri
     *            {@link URI} to which the link points to
     */
    public JHyperlink(String text, URI uri) {
        super();
        setup(text, uri);
    }

    /**
     * Constructor.
     * <p>
     * Constructs a {@link URI} object from the <code>uri</code> argument. Any {@link URISyntaxException} thrown is converted to {@link RuntimeException} if you cannot be sure at
     * compile time that your uri is valid, construct your uri manually and use the other constructor.
     * 
     * @param text
     *            text/label of the link
     * @param uri
     *            {@link URI} to which the link points to
     */
    public JHyperlink(String text, String uri) {
        super();
        URI oURI;
        try {
            oURI = new URI(uri);
        } catch (URISyntaxException e) {
            throw new RuntimeException(e);
        }
        setup(text, oURI);
    }

    /**
     * Sets the colour before the link has been clicked.
     * 
     * @param color
     *            {@link Color}
     */
    public void setColorBeforeClick(Color color) {
        this.colorBeforeClick = color;
        setForeground(color);
    }

    /**
     * Sets the colour once the link has been clicked.
     * 
     * @param color
     *            {@link Color}
     */
    public void setColorAfterClick(Color color) {
        this.colorAfterClick = color;
    }

    /**
     * Sets whether the text is underlined when the mouse hovers over the link
     * 
     * @param flag
     */
    public void setUnderlinedWhenHovered(boolean flag) {
        this.underlinedWhenHovered = flag;
    }

    /**
     * Sets whether the hand cursor is shown when the mouse hovers over the link
     * 
     * @param flag
     */
    public void setHandCursorWhenHovered(boolean flag) {
        this.handCursorWhenHovered = flag;
    }

    /**
     * @return the text/label displayed by the link
     */
    public String getLabel() {
        return text;
    }

    /**
     * @return the {@link URI} which the link points to
     */
    public URI getURI() {
        return uri;
    }

    /**
     * Sets up the link style, colour and mouse behaviour
     * 
     * @param text
     *            text/label of the link
     * @param uri
     *            {@link URI} to which the link points to
     */
    private void setup(final String text, final URI uri) {

        this.text = text;
        this.uri = uri;

        setText(text);
        setToolTipText(uri.toString());
        setForeground(colorBeforeClick);

        addMouseListener(new MouseAdapter() {

            @Override
            public void mouseClicked(MouseEvent e) {
                open(uri);
                setForeground(colorAfterClick);
            }

            @Override
            public void mouseEntered(MouseEvent e) {
                setText(text);
                mouseEntered = true;
                repaint();
                if (handCursorWhenHovered)
                    setCursor(new Cursor(Cursor.HAND_CURSOR));

            }

            @Override
            public void mouseExited(MouseEvent e) {
                setText(text);
                mouseEntered = false;
                repaint();
                if (handCursorWhenHovered)
                    setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

            }
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void paint(Graphics g) {

        super.paint(g);

        if (underlinedWhenHovered && mouseEntered) {

            // draw a line under the text
            Rectangle r = g.getClipBounds();
            g.drawLine(0, r.height - getFontMetrics(getFont()).getDescent(),
                    getFontMetrics(getFont()).stringWidth(getText()), r.height - getFontMetrics(getFont()).getDescent());
        }
    }

    /**
     * Opens the given {@link URI} in the default browser.
     * <p>
     * If the feature is not supported by the underlying system or if an error occur while launching the browser, an error message dialog is shown.
     * 
     * @param uri
     *            {@link URI} to which the link points to
     */
    private static void open(URI uri) {
        if (Desktop.isDesktopSupported()) {
            Desktop desktop = Desktop.getDesktop();
            try {
                desktop.browse(uri);
            } catch (IOException e) {
                JOptionPane.showMessageDialog(null, "Failed to launch the link, "
                        + "your computer is likely misconfigured.", "Cannot Launch Link", JOptionPane.WARNING_MESSAGE);
            }
        } else {
            JOptionPane.showMessageDialog(null, "Java is not able to launch links on your computer.",
                    "Cannot Launch Link", JOptionPane.WARNING_MESSAGE);
        }
    }

}
