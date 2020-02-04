/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2017 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
import static org.pdfsam.ui.help.HelpUtils.helpIcon;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.module.ModuleOwned;
import org.pdfsam.ui.support.Style;
import org.sejda.model.pdf.PdfVersion;

import javafx.scene.control.CheckBox;

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
        requireNotNullArg(constraint, "PdfVersion cannot be null");
        this.ownerModule = defaultString(ownerModule);
        this.constraint = constraint;
        this.setGraphic(helpIcon(DefaultI18nContext.getInstance().i18n("PDF version required: {0}",
                this.constraint.getVersionString())));
        this.getStyleClass().addAll(Style.WITH_HELP.css());

        selectedProperty().addListener((o, oldVal, newVal) -> {
            if (newVal) {
                eventStudio().broadcast(new AddPdfVersionConstraintEvent(constraint), ownerModule);
            } else {
                eventStudio().broadcast(new RemovePdfVersionConstraintEvent(constraint), ownerModule);
            }
        });
    }

    @Override
    public String getOwnerModule() {
        return ownerModule;
    }
}
