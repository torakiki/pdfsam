/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 25/lug/2014
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
package org.pdfsam.support;

import static org.pdfsam.support.RequireUtils.require;
import static org.pdfsam.support.RequireUtils.requireNotBlank;
import static org.pdfsam.support.RequireUtils.requireNotNull;
import static org.pdfsam.support.RequireUtils.requireState;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * @author Andrea Vacondio
 *
 */
public class RequireUtilsTest {

    @Rule
    public ExpectedException exception = ExpectedException.none();

    @Test
    public void nullArg() {
        exception.expect(IllegalArgumentException.class);
        exception.expectMessage("the message");
        requireNotNull(null, "the message");
    }

    @Test
    public void blankArg() {
        exception.expect(IllegalArgumentException.class);
        exception.expectMessage("the message");
        requireNotBlank(" ", "the message");
    }

    @Test
    public void falseArg() {
        exception.expect(IllegalArgumentException.class);
        exception.expectMessage("the message");
        require(false, "the message");
    }

    @Test
    public void illegalState() {
        exception.expect(IllegalStateException.class);
        exception.expectMessage("the message");
        requireState(false, "the message");
    }

    @Test
    public void positives() {
        requireNotNull(new Object(), "the message");
        requireNotBlank("I'm not blank", "the message");
        require(true, "the message");
        requireState(true, "the message");
    }
}
