/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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
package org.pdfsam.gui.components.content.preference;

import jakarta.inject.Inject;
import jakarta.inject.Named;
import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;
import org.pdfsam.ui.components.support.Style;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.pdfsam.ui.components.support.Views.helpIcon;

/**
 * Preference pane displaying the behavior section
 *
 * @author Andrea Vacondio
 */
class PreferenceBehaviorPane extends GridPane {

    @Inject
    public PreferenceBehaviorPane(@Named("checkForUpdates") PreferenceCheckBox checkForUpdates,
            @Named("playSounds") PreferenceCheckBox playSounds,
            @Named("donationNotification") PreferenceCheckBox donationNotification,
            @Named("fetchPremiumModules") PreferenceCheckBox fetchPremiumModules,
            CheckForUpdatesButton checkForUpdatesNow,
            @Named("logViewRowsNumber") PreferenceIntTextField logViewRowsNumber,
            @Named("checkForNews") PreferenceCheckBox checkForNews,
            @Named("clearConfirmation") PreferenceCheckBox clearConfirmation) {

        var logViewRowsLabel = new Label(i18n().tr("Log register rows") + ":");
        logViewRowsLabel.setLabelFor(logViewRowsNumber);
        add(logViewRowsLabel, 0, 1);
        setFillWidth(logViewRowsNumber, true);
        logViewRowsNumber.setMaxWidth(Double.POSITIVE_INFINITY);
        logViewRowsNumber.setAccessibleHelp(i18n().tr("Maximum number of rows displayed by the Log register"));
        add(logViewRowsNumber, 1, 1);
        add(helpIcon(i18n().tr("Maximum number of rows displayed by the Log register")), 2, 1);

        add(playSounds, 0, 2, 3, 1);
        add(donationNotification, 0, 3, 3, 1);
        add(checkForNews, 0, 4, 3, 1);
        add(fetchPremiumModules, 0, 5, 3, 1);
        add(clearConfirmation, 0, 6, 3, 1);
        add(new VBox(checkForUpdates, checkForUpdatesNow), 0, 7, 3, 1);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }

}
