/*
 * Created on 14/giu/2012
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.gui.preference;

import java.awt.Color;
import java.io.File;

import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.context.StringUserPreference;
import org.pdfsam.gui.view.base.BaseBrowsableField;

/**
 * Preference field where the value of the field can be browsed.
 * 
 * @author Andrea Vacondio
 * 
 */
abstract class BrowsableField extends BaseBrowsableField {

    private StringUserPreference preference;

    BrowsableField(String labelText, StringUserPreference preference) {
        super(labelText);
        setBackground(Color.WHITE);
        this.preference = preference;
    }

    @Override
    protected void onFileSelected(File selected) {
        DefaultUserContext.getInstance().setStringPreference(BrowsableField.this.preference,
                BrowsableField.this.getFieldValue());
    }
}
