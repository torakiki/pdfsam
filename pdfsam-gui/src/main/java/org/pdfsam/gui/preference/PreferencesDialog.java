/*
 * Created on 09/nov/2012
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
package org.pdfsam.gui.preference;

import java.awt.FlowLayout;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.EtchedBorder;

import org.pdfsam.configuration.PdfsamProperties;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.about.AboutDialog;

/**
 * Dialog showing preferences panel.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class PreferencesDialog extends JDialog {

    private PreferencesDialog() {
        setTitle(DefaultI18nContext.getInstance().i18n("Preferences"));
        init();
    }

    private void init() {
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setIconImage(new ImageIcon(AboutDialog.class.getResource("/images/pdfsam_" + PdfsamProperties.PACKAGE + ".png"))
                .getImage());
        setSize(540, 580);
        setLayout(new GridBagLayout());

        GridBagConstraints c = new GridBagConstraints();

        c.ipady = 10;
        c.ipadx = 10;
        c.gridwidth = 3;
        c.gridheight = 2;
        c.gridx = 0;
        c.gridy = 0;
        c.weightx = 1;
        c.weighty = 1;
        c.fill = GridBagConstraints.BOTH;
        JScrollPane scroll = new JScrollPane();
        scroll.setViewportView(new PreferencesPanel());
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

    /**
     * Close action for the frame
     * 
     * @author Andrea Vacondio
     * 
     */
    private final class CloseAction extends AbstractAction {

        private CloseAction() {
            super(DefaultI18nContext.getInstance().i18n("Close"));
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
    private static final class PreferencesFrameHolder {

        private PreferencesFrameHolder() {
            // hide constructor
        }

        static final PreferencesDialog PREFS_FRAME = new PreferencesDialog();
    }

    public static PreferencesDialog getInstance() {
        return PreferencesFrameHolder.PREFS_FRAME;
    }
}
