/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/ott/2014
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
package org.pdfsam.ui.components.selection;

import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Labeled;
import javafx.scene.control.Tooltip;
import org.kordamp.ikonli.javafx.FontIcon;
import org.pdfsam.model.pdf.PdfDescriptorLoadingStatus;
import org.sejda.commons.util.StringUtils;

import java.util.Arrays;
import java.util.Optional;
import java.util.function.Consumer;

import static java.util.Objects.nonNull;
import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Consumer taking care of updating a {@link Labeled} indicator based on the input {@link PdfDescriptorLoadingStatus}
 * 
 * @author Andrea Vacondio
 *
 */
public class LoadingStatusIndicatorUpdater implements Consumer<PdfDescriptorLoadingStatus> {

    private final Labeled indicator;

    public LoadingStatusIndicatorUpdater(Labeled indicator) {
        requireNotNullArg(indicator, "Cannot set loading status on a null indicator");
        this.indicator = indicator;
    }

    @Override
    public void accept(PdfDescriptorLoadingStatus t) {

        var icon = Optional.ofNullable(t).map(PdfDescriptorLoadingStatus::getIcon).map(FontIcon::of).orElse(null);
        if (nonNull(icon)) {
            indicator.setGraphic(icon);
            indicator.setContentDisplay(ContentDisplay.CENTER);
        } else {
            indicator.setGraphic(null);
        }
        Arrays.stream(PdfDescriptorLoadingStatus.values()).map(PdfDescriptorLoadingStatus::getStyle)
                .filter(StringUtils::isNotEmpty).forEach(indicator.getStyleClass()::remove);
        if (nonNull(t) && isNotBlank(t.getStyle())) {
            indicator.getStyleClass().add(t.getStyle());
        }
        if (nonNull(t) && isNotBlank(t.getDescription())) {
            indicator.setTooltip(new Tooltip(t.getDescription()));
            indicator.setAccessibleText(t.getDescription());
        } else {
            indicator.setTooltip(null);
            indicator.setAccessibleText(null);
        }
    }
}
