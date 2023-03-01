/*
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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

import javafx.scene.control.ComboBox;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.model.ui.ComboItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Combo box that sets a {@link StringPersistentProperty}
 *
 * @param <T> the type of the elements in the combo
 * @author Andrea Vacondio
 */
public class PreferenceComboBox<T extends ComboItem<String>> extends ComboBox<T> {
    private static final Logger LOG = LoggerFactory.getLogger(PreferenceComboBox.class);

    PreferenceComboBox(StringPersistentProperty property) {
        this(property, app());
    }

    PreferenceComboBox(StringPersistentProperty property, ApplicationContext context) {
        requireNotNullArg(property, "Preference cannot be null");
        valueProperty().addListener((observable, oldValue, newValue) -> {
            context.persistentSettings().set(property, newValue.key());
            LOG.trace("Preference {} set to {}", property, newValue.key());
        });
    }
}
