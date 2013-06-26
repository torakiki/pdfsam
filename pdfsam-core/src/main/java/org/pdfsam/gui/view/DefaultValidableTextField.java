/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/giu/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.view;

/**
 * Default implementation of an {@link AbstractValidableTextField} where an empty callback is defined.
 * 
 * @author Andrea Vacondio
 * 
 */
public class DefaultValidableTextField extends AbstractValidableTextField {

    @Override
    protected void onValidInput() {
        // do nothing
    }

}
