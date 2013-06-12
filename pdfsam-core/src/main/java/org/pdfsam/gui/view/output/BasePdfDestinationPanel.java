/*
 * Created on 10/feb/2013
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
package org.pdfsam.gui.view.output;

import javax.swing.JPanel;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;
import org.pdfsam.gui.view.Views;
import org.pdfsam.gui.view.base.PdfVersionConstrainedCheckbox;
import org.pdfsam.support.filter.FileFilterType;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * A Panel where the user can select a destination pdf file
 * 
 * @author Andrea Vacondio
 * 
 */
class BasePdfDestinationPanel extends DestinationPanel implements WithEventNamespace {

    private EventNamespace namespace = EventNamespace.NULL;
    private PdfVersionCombo combo = new PdfVersionCombo();
    private PdfVersionConstrainedCheckbox compress = Views.newCompressOutputCheckbox();

    public BasePdfDestinationPanel(FileFilterType filterType, int chooserMode) {
        super(filterType, chooserMode);
        JPanel comboPanel = Views.newLabeledComponent(combo,
                DefaultI18nContext.getInstance().i18n("Output document pdf version:"), EMPTY);

        compress.setAlignmentX(LEFT_ALIGNMENT);
        add(compress);
        comboPanel.setAlignmentX(LEFT_ALIGNMENT);
        add(comboPanel);
    }

    public void setEventNamespace(EventNamespace namespace) {
        this.namespace = namespace;
        this.combo.setEventNamespace(namespace);
        this.compress.setEventNamespace(namespace);
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }

}
