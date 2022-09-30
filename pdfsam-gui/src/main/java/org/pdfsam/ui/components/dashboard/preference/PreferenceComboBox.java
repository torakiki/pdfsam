/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
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
package org.pdfsam.ui.components.dashboard.preference;

import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

import org.pdfsam.core.context.StringUserPreference;
import org.pdfsam.core.context.UserContext;
import org.pdfsam.core.support.KeyValueItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javafx.scene.control.ComboBox;

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
        requireNotNullArg(preference, "Preference cannot be null");
        requireNotNullArg(userContext, "UserContext cannot be null");
        this.preference = preference;
        valueProperty().addListener((observable, oldValue, newValue) -> {
            userContext.setStringPreference(PreferenceComboBox.this.preference, newValue.getKey());
            LOG.trace("Preference {} set to {}", PreferenceComboBox.this.preference, newValue.getKey());
        });
    }
}
