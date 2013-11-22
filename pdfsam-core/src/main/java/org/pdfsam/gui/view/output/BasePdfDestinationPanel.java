/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/feb/2013
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
package org.pdfsam.gui.view.output;

import javax.swing.JPanel;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.String;
import org.pdfsam.gui.view.PdfVersionConstrainedCheckbox;
import org.pdfsam.gui.view.Views;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.support.filter.FileFilterType;

import static org.apache.commons.lang3.StringUtils.EMPTY;

/**
 * A Panel where the user can select a destination pdf file
 * 
 * @author Andrea Vacondio
 * 
 */
class BasePdfDestinationPanel extends DestinationPanel implements ModuleOwned {

    private String namespace = String.NULL;
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

    public void setEventNamespace(String namespace) {
        this.namespace = namespace;
        this.combo.setEventNamespace(namespace);
        this.compress.setEventNamespace(namespace);
    }

    public String getOwnerModule() {
        return namespace;
    }

}
