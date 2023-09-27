/*
 * This file is part of the PDF Split And Merge source code
 * Created on 22 ott 2016
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
package org.pdfsam.core.support.validation;

import java.util.HashSet;
import java.util.Set;

import static java.util.Optional.ofNullable;

/**
 * Validates that a given String represent an integer in a given set
 * 
 * @author Andrea Vacondio
 *
 */
public class ContainedIntegerValidator implements Validator<String> {
    private final Set<Integer> valid = new HashSet<>();

    public ContainedIntegerValidator(Set<Integer> validValues) {
        ofNullable(validValues).map(valid::addAll);
    }

    @Override
    public boolean isValid(String input) {
        try {
            return valid.contains(Integer.parseInt(input));
        } catch (NumberFormatException e) {
            // not a valid integer
            return false;
        }
    }

}
