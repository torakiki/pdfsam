/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 09/giu/2014
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
package org.pdfsam.support.validation;

import static org.apache.commons.lang3.StringUtils.isNotBlank;
import static org.sejda.commons.util.RequireUtils.requireNotBlank;

import java.util.regex.Pattern;

/**
 * Validator of a string against a given regex. null string is considered as valid.
 * 
 * @author Andrea Vacondio
 *
 */
class RegexValidator implements Validator<String> {

    private Pattern pattern;

    public RegexValidator(String regex) {
        requireNotBlank(regex, "Regex cannot be blank");
        pattern = Pattern.compile(regex);
    }

    @Override
    public boolean isValid(String input) {
        return isNotBlank(input) && pattern.matcher(input).matches();
    }

}
