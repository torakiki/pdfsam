/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
package org.pdfsam.ui.dashboard.preference;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.i18n.I18nContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 * Preference pane displaying the Thumbnails section
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceThumbnailsPane extends VBox {

    @Inject
    public PreferenceThumbnailsPane(
            @Named("thumbnailsCombo") PreferenceComboBox<KeyStringValueItem<String>> thumbnailsCombo,
            @Named("highQualityThumbnails") PreferenceCheckBox highQualityThumbnails,
            @Named("thumbnailsSize") PreferenceIntTextField thumbSize) {
        I18nContext i18n = DefaultI18nContext.getInstance();

        highQualityThumbnails.setTooltip(new Tooltip(i18n.i18n("Generate high quality thumbnails (slower)")));
        highQualityThumbnails.getStyleClass().add("spaced-vitem");

        HBox second = new HBox(new Label(i18n.i18n("Size in px:")), thumbSize);
        second.getStyleClass().addAll(Style.HCONTAINER.css());
        second.getStyleClass().addAll(Style.VITEM.css());

        thumbnailsCombo.setTooltip(new Tooltip(i18n.i18n("Library used to generate thumbnails")));
        getChildren().addAll(highQualityThumbnails, second, new Label(i18n.i18n("Thumbnails creator:")),
                thumbnailsCombo);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
