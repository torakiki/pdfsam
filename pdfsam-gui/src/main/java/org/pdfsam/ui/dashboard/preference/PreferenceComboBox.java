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

import static org.pdfsam.support.RequireUtils.requireNotNull;
import javafx.scene.control.ComboBox;

import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.KeyValueItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Combo box that sets a {@link StringUserPreference}
 * 
 * @author Andrea Vacondio
 * @param <T>
 *            the type of the elements in the combo
 */
public class PreferenceComboBox<T extends KeyValueItem<String, String>> extends ComboBox<T> {
    private static final Logger LOG = LoggerFactory.getLogger(PreferenceComboBox.class);

    private StringUserPreference preference;

    public PreferenceComboBox(StringUserPreference preference, UserContext userContext) {
        requireNotNull(preference, "Preference cannot be null");
        requireNotNull(userContext, "UserContext cannot be null");
        this.preference = preference;
        valueProperty().addListener((observable, oldValue, newValue) -> {
            userContext.setStringPreference(PreferenceComboBox.this.preference, newValue.getKey());
            LOG.trace("Preference {} set to {}", PreferenceComboBox.this.preference, newValue.getKey());
        });
    }
}
