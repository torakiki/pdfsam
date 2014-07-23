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

import javafx.scene.control.Label;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

import javax.annotation.PostConstruct;
import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.I18nContext;
import org.pdfsam.context.IntUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.KeyStringValueItem;
import org.pdfsam.support.validation.Validators;
import org.pdfsam.ui.commons.ValidableTextField;
import org.pdfsam.ui.support.FXValidationSupport.ValidationState;
import org.pdfsam.ui.support.Style;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Preference pane displaying the Thumbnails section
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class PreferenceThumbnailsPane extends VBox {
    private static final Logger LOG = LoggerFactory.getLogger(PreferenceThumbnailsPane.class);

    private static final Integer LOWER = 130;
    private static final Integer UPPER = 390;

    @Inject
    private UserContext userContext;
    @Inject
    @Named("thumbnailsCombo")
    private PreferenceComboBox<KeyStringValueItem<String>> thumbnailsCombo;
    @Inject
    @Named("highQualityThumbnails")
    private PreferenceCheckBox highQualityThumbnails;

    @PostConstruct
    public void post() {
        I18nContext i18n = DefaultI18nContext.getInstance();

        highQualityThumbnails.setTooltip(new Tooltip(i18n.i18n("Generate high quality thumbnails (slower)")));
        highQualityThumbnails.getStyleClass().add("spaced-vitem");

        final ValidableTextField thumbSize = new ValidableTextField(Integer.toString(userContext.getThumbnailsSize()));
        thumbSize.setValidator(Validators.newIntRangeString(LOWER, UPPER));
        thumbSize.setEnableInvalidStyle(true);
        thumbSize
                .setErrorMessage(i18n.i18n("Size must be between {0}px and {1}px", LOWER.toString(), UPPER.toString()));
        String helpText = i18n.i18n("Pixel size of the thumbnails (between {0}px and {1}px)", LOWER.toString(),
                UPPER.toString());
        thumbSize.setOnEnterValidation(true);
        thumbSize.setPromptText(helpText);
        thumbSize.setTooltip(new Tooltip(helpText));
        thumbSize.validProperty().addListener(
                (o, oldVal, newVal) -> {
                    if (newVal == ValidationState.VALID) {
                        userContext.setIntegerPreference(IntUserPreference.THUMBNAILS_SIZE,
                                Integer.parseInt(thumbSize.getText()));
                        LOG.trace("Preference {} set to {}", IntUserPreference.THUMBNAILS_SIZE, thumbSize.getText());
                    }
                });
        HBox second = new HBox(new Label(i18n.i18n("Size in px:")), thumbSize);
        second.getStyleClass().addAll(Style.HCONTAINER.css());
        second.getStyleClass().addAll(Style.VITEM.css());

        thumbnailsCombo.setTooltip(new Tooltip(i18n.i18n("Library used to generate thumbnails")));
        getChildren().addAll(highQualityThumbnails, second, new Label(i18n.i18n("Thumbnails creator:")),
                thumbnailsCombo);
        getStyleClass().addAll(Style.CONTAINER.css());
    }
}
