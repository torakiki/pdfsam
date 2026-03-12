/*
 * This file is part of the PDF Split And Merge source code
 * Created on 25/nov/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@soberlemur.com).
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
package org.pdfsam.ui.components.io;

import javafx.scene.control.CheckBox;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.model.ui.AddPdfVersionConstraintEvent;
import org.pdfsam.model.ui.RemovePdfVersionConstraintEvent;
import org.pdfsam.ui.components.support.Style;
import org.sejda.model.pdf.PdfVersion;

import static org.apache.commons.lang3.StringUtils.defaultString;
import static org.pdfsam.eventstudio.StaticStudio.eventStudio;
import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * A checkbox that, when ticked, informs other component that a constraint on the output pdf document version has to be enforced.
 *
 * @author Andrea Vacondio
 */
class PdfVersionConstrainedCheckBox extends CheckBox implements ToolBound {
    private final PdfVersion constraint;
    private String toolBinding = StringUtils.EMPTY;

    public PdfVersionConstrainedCheckBox(PdfVersion constraint, String toolBinding) {
        requireNotNullArg(constraint, "PdfVersion cannot be null");
        this.toolBinding = defaultString(toolBinding);
        this.constraint = constraint;
        this.setGraphic(helpIcon(i18n().tr("PDF version required: {0}", this.constraint.getVersionString())));
        this.getStyleClass().addAll(Style.WITH_HELP.css());
        setAccessibleHelp(i18n().tr("When selected, requires PDF version {0} or higher for the output file",
                this.constraint.getVersionString()));

        selectedProperty().addListener((o, oldVal, newVal) -> {
            if (newVal) {
                eventStudio().broadcast(new AddPdfVersionConstraintEvent(constraint), toolBinding);
            } else {
                eventStudio().broadcast(new RemovePdfVersionConstraintEvent(constraint), toolBinding);
            }
        });
    }

    @Override
    public String toolBinding() {
        return toolBinding;
    }
}
