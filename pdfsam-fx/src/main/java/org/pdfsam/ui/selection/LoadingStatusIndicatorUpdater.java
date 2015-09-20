/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.selection;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.function.Consumer;

import org.pdfsam.pdf.PdfDescriptorLoadingStatus;

import javafx.scene.control.Labeled;
import javafx.scene.control.Tooltip;

/**
 * Consumer taking care of updating a {@link Labeled} indicator based on the input {@link PdfDescriptorLoadingStatus}
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusIndicatorUpdater implements Consumer<PdfDescriptorLoadingStatus> {

    private Labeled indicator;

    public LoadingStatusIndicatorUpdater(Labeled indicator) {
        requireNotNull(indicator, "Cannot set loading status on a null indicator");
        this.indicator = indicator;
    }

    public void accept(PdfDescriptorLoadingStatus t) {
        indicator.setText(textValueFor(t));
        if (t != null && isNotBlank(t.getDescription())) {
            indicator.setTooltip(new Tooltip(t.getDescription()));
        } else {
            indicator.setTooltip(null);
        }

    }

    public static String textValueFor(PdfDescriptorLoadingStatus item) {
        return (item != null && item.getIcon() != null) ? item.getIcon().toString() : "";
    }
}
