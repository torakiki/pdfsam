/*
 * Created on 07/nov/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.about;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EtchedBorder;

import org.pdfsam.Pdfsam;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Frame to display About informations
 * 
 * @author Andrea Vacondio
 * 
 */
public final class AboutFrame extends JFrame {

    private AboutFrame() {
        super(DefaultI18nContext.getInstance().getI18n().tr("About"));
        init();
    }

    private void init() {
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setIconImage(new ImageIcon(AboutFrame.class.getResource("/images/pdfsam_" + Pdfsam.PACKAGE + ".png"))
                .getImage());
        setSize(460, 210);
        setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();
        c.ipady = 20;
        c.ipadx = 20;
        c.gridwidth = 1;
        c.gridheight = 2;
        c.gridx = 0;
        c.gridy = 0;
        c.weighty = 1;
        c.fill = GridBagConstraints.VERTICAL;
        add(imagePanel(), c);

        c.ipady = 10;
        c.ipadx = 10;
        c.gridwidth = 2;
        c.gridheight = 2;
        c.gridx = 1;
        c.gridy = 0;
        c.weightx = 1;
        c.weighty = 1;
        c.fill = GridBagConstraints.BOTH;
        JScrollPane scroll = new JScrollPane();
        scroll.setViewportView(new AboutPanel());
        scroll.setBorder(BorderFactory.createEmptyBorder());
        add(scroll, c);

        c.ipady = 0;
        c.ipadx = 0;
        c.gridwidth = 3;
        c.gridheight = 1;
        c.gridx = 0;
        c.gridy = 2;
        c.weightx = 0;
        c.weighty = 0;
        c.fill = GridBagConstraints.HORIZONTAL;
        add(buttonPanel(), c);
    }

    private JPanel buttonPanel() {
        JPanel buttonPanel = new JPanel();
        buttonPanel.setLayout(new FlowLayout(FlowLayout.RIGHT, 5, 5));
        buttonPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
        JButton closeButton = new JButton(new CloseAction());
        buttonPanel.add(closeButton);
        return buttonPanel;
    }

    private JPanel imagePanel() {
        JLabel image = new JLabel(new ImageIcon(AboutFrame.class.getResource("/images/pdfsam_" + Pdfsam.PACKAGE
                + "_128.png")));
        image.setMinimumSize(new Dimension(128, 128));
        JPanel imagePanel = new JPanel();
        imagePanel.setLayout(new BoxLayout(imagePanel, BoxLayout.Y_AXIS));
        Dimension fillerSize = new Dimension(10, 0);
        imagePanel.add(new Box.Filler(fillerSize, fillerSize, fillerSize));
        imagePanel.add(Box.createVerticalGlue());
        imagePanel.setBackground(Color.WHITE);
        imagePanel.add(image);
        imagePanel.add(Box.createVerticalGlue());
        return imagePanel;
    }

    /**
     * Close action for the frame
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class CloseAction extends AbstractAction {

        private CloseAction() {
            super(DefaultI18nContext.getInstance().getI18n().tr("Close"));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            getInstance().setVisible(false);

        }

    }

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class AboutFrameHolder {

        private AboutFrameHolder() {
            // hide constructor
        }

        static final AboutFrame ABOUT_FRAME = new AboutFrame();
    }

    public static AboutFrame getInstance() {
        return AboutFrameHolder.ABOUT_FRAME;
    }
}
