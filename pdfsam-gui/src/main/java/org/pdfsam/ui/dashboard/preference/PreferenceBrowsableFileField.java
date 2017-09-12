/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/ott/2013
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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

import org.pdfsam.context.StringUserPreference;
import org.pdfsam.context.UserContext;
import org.pdfsam.support.io.FileType;
import org.pdfsam.ui.io.BrowsableFileField;
import org.pdfsam.ui.io.RememberingLatestFileChooserWrapper.OpenType;

/**
 * {@link BrowsableFileField} that sets a {@link StringUserPreference} when the input text is valid.
 * 
 * @author Andrea Vacondio
 * 
 */
class PreferenceBrowsableFileField extends BrowsableFileField {

    PreferenceBrowsableFileField(StringUserPreference preference, FileType type, OpenType openType,
            UserContext userContext) {
        super(type, openType);
        requireNotNull(preference, "Preference cannot be null");
        requireNotNull(userContext, "UserContext cannot be null");
        enforceValidation(true, true);
        getTextField().validProperty().addListener(
                new PreferenceSetterOnValidState(preference, getTextField(), userContext));
    }

}
