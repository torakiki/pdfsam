/*
 * Created on 19/giu/2013
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

import javax.swing.Action;
import javax.swing.JButton;

import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.bushe.swing.event.annotation.EventSubscriber;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.pdf.PdfLoadCompletedEvent;
import org.pdfsam.pdf.PdfLoadRequestEvent;

/**
 * Button displayed in the selection table toolbar
 * 
 * @author Andrea Vacondio
 * 
 */
public class SelectionToolbarButton extends JButton implements WithEventNamespace {
    private EventNamespace namespace = EventNamespace.NULL;

    public SelectionToolbarButton(Action a, EventNamespace namespace) {
        super(a);
        this.namespace = namespace;
        AnnotationProcessor.process(this);
    }

    @EventSubscriber
    public void disableWhileLoadingDocuments(PdfLoadRequestEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            // I'm still loading documents
            setEnabled(false);
        }
    }

    @EventSubscriber
    public void enableOnLoadDocumentsCompletion(PdfLoadCompletedEvent event) {
        if (event.getNamespace().isParentOf(getEventNamespace())) {
            // I'm done loading documents
            setEnabled(true);
        }
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }
}
