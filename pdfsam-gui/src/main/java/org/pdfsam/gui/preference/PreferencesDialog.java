/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/nov/2012
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
package org.pdfsam.gui.preference;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.event.ActionEvent;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;
import javax.swing.AbstractAction;
import javax.swing.BorderFactory;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JScrollPane;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.view.Views;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Dialog showing preferences panel.
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
public final class PreferencesDialog extends JDialog {

    @Inject
    @Qualifier("pdfsamLogoImage")
    private Image image;

    private PreferencesDialog() {
        setTitle(DefaultI18nContext.getInstance().i18n("Preferences"));
    }

    @PostConstruct
    void init() {
        setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        setIconImage(image);
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
            PreferencesDialog.this.setVisible(false);
        }

    }

}
