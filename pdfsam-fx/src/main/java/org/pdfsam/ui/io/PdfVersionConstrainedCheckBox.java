/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.io;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.ui.support.Style;
import org.sejda.model.pdf.PdfVersion;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Tooltip;

/**
 * A checkbox that, when ticked, informs other component that a constraint on the output pdf document version has to be enforced.
 * 
 * @author Andrea Vacondio
 * 
 */
class PdfVersionConstrainedCheckBox extends CheckBox implements ModuleOwned {
    private PdfVersion constraint;
    private String ownerModule = StringUtils.EMPTY;

    public PdfVersionConstrainedCheckBox(PdfVersion constraint, String ownerModule) {
        requireNotNull(constraint, "PdfVersion cannot be null");
        this.ownerModule = defaultString(ownerModule);
        this.constraint = constraint;
        setTooltip(new Tooltip(DefaultI18nContext.getInstance().i18n("PDF version required: {0}",
                Double.toString(this.constraint.getVersionAsDouble()))));

        selectedProperty().addListener((o, oldVal, newVal) -> {
            if (newVal) {
                eventStudio().broadcast(new AddPdfVersionConstraintEvent(constraint), ownerModule);
            } else {
                eventStudio().broadcast(new RemovePdfVersionConstraintEvent(constraint), ownerModule);
            }
        });

        getStyleClass().addAll(Style.VITEM.css());
    }

    public String getOwnerModule() {
        return ownerModule;
    }
}
