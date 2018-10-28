/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
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
package org.pdfsam.ui.selection;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.Optional;
import java.util.function.Consumer;

import org.pdfsam.pdf.PdfDescriptorLoadingStatus;

import de.jensd.fx.glyphs.fontawesome.FontAwesomeIcon;
import de.jensd.fx.glyphs.fontawesome.utils.FontAwesomeIconFactory;
import javafx.scene.control.ContentDisplay;
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

    @Override
    public void accept(PdfDescriptorLoadingStatus t) {

        FontAwesomeIcon icon = Optional.ofNullable(t).map(PdfDescriptorLoadingStatus::getIcon).orElse(null);
        if (nonNull(icon)) {
            FontAwesomeIconFactory.get().setIcon(indicator, icon, ContentDisplay.CENTER);
        } else {
            indicator.setGraphic(null);
        }
        if (t != null && isNotBlank(t.getDescription())) {
            indicator.setTooltip(new Tooltip(t.getDescription()));
        } else {
            indicator.setTooltip(null);
        }
    }
}
