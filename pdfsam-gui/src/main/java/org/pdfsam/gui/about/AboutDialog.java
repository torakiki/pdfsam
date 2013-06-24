/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.about;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.pdfsam.configuration.PdfsamProperties;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.view.Views;

/**
 * Dialog to display About informations.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class AboutDialog extends JDialog {

    private AboutDialog() {
        setTitle(DefaultI18nContext.getInstance().i18n("About"));
        init();
    }

    private void init() {
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setIconImage(new ImageIcon(AboutDialog.class.getResource("/images/pdfsam_" + PdfsamProperties.PACKAGE + ".png"))
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
        add(new AboutImagePanel(), c);

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
        add(Views.newButtonsPanel(new CloseAction()), c);
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
    private static final class AboutFrameHolder {

        private AboutFrameHolder() {
            // hide constructor
        }

        static final AboutDialog ABOUT_FRAME = new AboutDialog();
    }

    public static AboutDialog getInstance() {
        return AboutFrameHolder.ABOUT_FRAME;
    }
}
