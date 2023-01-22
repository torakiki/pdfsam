/*
 * This file is part of the PDF Split And Merge source code
 * Created on 30/ott/2013
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

import org.pdfsam.core.context.ApplicationContext;
import org.pdfsam.core.context.StringPersistentProperty;
import org.pdfsam.ui.components.io.BrowsableDirectoryField;

import static org.pdfsam.core.context.ApplicationContext.app;
import static org.pdfsam.core.support.validation.Validators.existingDirectory;
import static org.pdfsam.core.support.validation.Validators.validEmpty;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * {@link BrowsableDirectoryField} that sets a {@link StringPersistentProperty} when the input text is valid.
 *
 * @author Andrea Vacondio
 */
class PreferenceBrowsableDirectoryField extends BrowsableDirectoryField {

    PreferenceBrowsableDirectoryField(StringPersistentProperty property) {
        this(property, app());
    }

    PreferenceBrowsableDirectoryField(StringPersistentProperty property, ApplicationContext context) {
        getTextField().setValidator(validEmpty(existingDirectory()));
        requireNotNullArg(property, "Preference cannot be null");
        getTextField().validProperty().addListener(new PreferenceSetterOnValidState(property, getTextField(), context));
    }
}
