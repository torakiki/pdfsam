/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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

import static org.pdfsam.ui.help.HelpUtils.helpIcon;

import javax.inject.Inject;
import javax.inject.Named;

import org.pdfsam.i18n.DefaultI18nContext;
import org.pdfsam.ui.support.Style;

import javafx.scene.control.Label;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.VBox;

/**
 * Preference pane displaying the behavior section
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceBehaviorPane extends GridPane {

    @Inject
    public PreferenceBehaviorPane(@Named("checkForUpdates") PreferenceCheckBox checkForUpdates,
            @Named("playSounds") PreferenceCheckBox playSounds,
            @Named("donationNotification") PreferenceCheckBox donationNotification,
            @Named("fetchPremiumModules") PreferenceCheckBox fetchPremiumModules,
            CheckForUpdatesButton checkForUpdatesNow,
            @Named("logViewRowsNumber") PreferenceIntTextField logViewRowsNumber,
            @Named("checkForNews") PreferenceCheckBox checkForNews) {

        add(new Label(DefaultI18nContext.getInstance().i18n("Log register rows:")), 0, 1);
        setFillWidth(logViewRowsNumber, true);
        logViewRowsNumber.setMaxWidth(Double.POSITIVE_INFINITY);
        add(logViewRowsNumber, 1, 1);
        add(helpIcon(DefaultI18nContext.getInstance().i18n("Maximum number of rows displayed by the Log register")), 2,
                1);

        add(playSounds, 0, 2, 3, 1);
        add(donationNotification, 0, 3, 3, 1);
        add(checkForNews, 0, 4, 3, 1);
        add(fetchPremiumModules, 0, 5, 3, 1);
        add(new VBox(checkForUpdates, checkForUpdatesNow), 0, 6, 3, 1);

        getStyleClass().addAll(Style.CONTAINER.css());
        getStyleClass().addAll(Style.GRID.css());
    }

}
