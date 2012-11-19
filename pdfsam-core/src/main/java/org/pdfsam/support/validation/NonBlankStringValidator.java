/*
 * Created on 16/nov/2012
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
package org.pdfsam.support.validation;

import org.apache.commons.lang3.StringUtils;

/**
 * Validates that the input string is not blank, empty or null.
 * 
 * @author Andrea Vacondio
 * 
 */
class NonBlankStringValidator implements Validator<String> {

    public boolean isValid(String input) {
        return StringUtils.isNotBlank(input);
    }

}
