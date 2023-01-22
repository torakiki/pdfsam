/*
 * This file is part of the PDF Split And Merge source code
 * Created on 13/giu/2012
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
package org.pdfsam.gui.components.content.preference;

import javafx.scene.control.CheckBox;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Checkbox updating the relative preference on item selection/deselection
 *
 * @author Andrea Vacondio
 */
class PreferenceCheckBox extends CheckBox {

    private static final Logger LOG = LoggerFactory.getLogger(PreferenceCheckBox.class);

    PreferenceCheckBox(BooleanPersistentProperty property, String label, boolean selected) {
        this(property, label, selected, app());
    }

    PreferenceCheckBox(BooleanPersistentProperty property, String label, boolean selected, ApplicationContext context) {
        super(label);
        requireNotNullArg(property, "Preference cannot be null");
        setSelected(selected);
        selectedProperty().addListener((ov, oldVal, newVal) -> {
            context.persistentSettings().set(property, newVal);
            LOG.trace("Preference {} set to {}", property, newVal);
        });
    }
}
