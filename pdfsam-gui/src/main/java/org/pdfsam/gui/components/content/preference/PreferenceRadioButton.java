/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/giu/2014
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

import javafx.scene.control.RadioButton;
import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.BooleanPersistentProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * RadioButton updating the relative preference on item selection/deselection
 *
 * @author Andrea Vacondio
 */
class PreferenceRadioButton extends RadioButton {
    private static final Logger LOG = LoggerFactory.getLogger(PreferenceRadioButton.class);

    PreferenceRadioButton(BooleanPersistentProperty property, String label, boolean selected) {
        this(property, label, selected, app());
    }

    PreferenceRadioButton(BooleanPersistentProperty property, String label, boolean selected,
            ApplicationContext context) {
        super(label);
        requireNotNullArg(property, "Preference cannot be null");
        requireNotNullArg(context, "ApplicationContext cannot be null");
        setSelected(selected);
        selectedProperty().addListener((ov, oldVal, newVal) -> {
            context.persistentSettings().set(property, newVal);
            LOG.trace("Preference {} set to {}", property, newVal);
        });

    }
}
