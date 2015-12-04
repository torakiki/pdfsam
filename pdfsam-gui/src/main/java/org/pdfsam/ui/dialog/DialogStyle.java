/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 03 dic 2015
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.ui.dialog;

import de.jensd.fx.glyphs.materialdesignicons.MaterialDesignIcon;

/**
 * Possible dialog styles
 * 
 * @author Andrea Vacondio
 */
public enum DialogStyle {
    WARNING("-pdfsam-warning-dialog", MaterialDesignIcon.ALERT),
    QUESTION("-pdfsam-question-dialog", MaterialDesignIcon.HELP_CIRCLE);
    public final String style;
    public final MaterialDesignIcon icon;

    private DialogStyle(String style, MaterialDesignIcon icon) {
        this.style = style;
        this.icon = icon;
    }

}
