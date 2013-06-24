/*
 * Created on 15/giu/2013
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
package org.pdfsam.gui.view.selection;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.KeyStroke;

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.SharedJFileChooser;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfDocumentDescriptor;
import org.pdfsam.pdf.PdfLoadRequestEvent;
import org.pdfsam.support.filter.FileFilterType;

/**
 * Action to perform pdf documents loading
 * 
 * @author Andrea Vacondio
 * 
 */
class AddAction extends AbstractAction implements WithEventNamespace {

    private EventNamespace namespace;

    public AddAction(EventNamespace namespace) {
        super(DefaultI18nContext.getInstance().i18n("Add"));
        this.namespace = namespace;
        this.setEnabled(true);
        this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_A, InputEvent.ALT_DOWN_MASK));
        this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance().i18n("Add documents to the table"));
        this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/add.png")));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        JFileChooser chooser = SharedJFileChooser.getInstance(FileFilterType.PDF, JFileChooser.FILES_AND_DIRECTORIES);
        chooser.setMultiSelectionEnabled(true);
        int retVal = chooser.showOpenDialog(null);

        if (retVal == JFileChooser.APPROVE_OPTION) {
            PdfLoadRequestEvent loadEvent = new PdfLoadRequestEvent(namespace);
            for (File current : chooser.getSelectedFiles()) {
                loadEvent.add(PdfDocumentDescriptor.newDescriptorNoPassword(current));
            }
            EventBus.publish(loadEvent);
        }

    }

    @Override
    public EventNamespace getEventNamespace() {
        return namespace;
    }
}
