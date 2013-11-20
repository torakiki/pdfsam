/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 08/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.view;

import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;

import javax.swing.JCheckBox;

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.AddPdfVersionConstraintEvent;
import org.pdfsam.gui.event.String;
import org.pdfsam.gui.event.RemovePdfVersionConstraintEvent;
import org.pdfsam.gui.event.ModuleOwned;
import org.sejda.model.pdf.PdfVersion;

import static org.pdfsam.support.RequireUtils.requireNotNull;

/**
 * A checkbox that imposes constraints on the pdf version of the output document.
 * 
 * @author Andrea Vacondio
 * 
 */
public class PdfVersionConstrainedCheckbox extends JCheckBox implements ModuleOwned {

    private PdfVersion constraint;
    private String namespace = String.NULL;

    public PdfVersionConstrainedCheckbox(PdfVersion constraint) {
        requireNotNull(constraint, "PdfVersion cannot be null");
        this.constraint = constraint;
        addItemListener(new ItemStateChangedNotifier());
        setToolTipText(DefaultI18nContext.getInstance().i18n("Pdf version required: {0}",
                Double.toString(this.constraint.getVersionAsDouble())));
    }

    public void setEventNamespace(String namespace) {
        this.namespace = namespace;
    }

    @Override
    public String getOwnerModule() {
        return namespace;
    }

    /**
     * Listener notifying subscribers that the constraint imposed by this checkbox is active/inactive
     * 
     * @author Andrea Vacondio
     * 
     */
    private class ItemStateChangedNotifier implements ItemListener {
        public void itemStateChanged(ItemEvent e) {
            if (e.getStateChange() == ItemEvent.SELECTED) {
                EventBus.publish(new AddPdfVersionConstraintEvent(namespace, constraint));
            } else {
                EventBus.publish(new RemovePdfVersionConstraintEvent(namespace, constraint));
            }
        }
    }
}
