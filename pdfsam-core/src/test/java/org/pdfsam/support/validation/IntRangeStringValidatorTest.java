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

import org.junit.Assert;
import org.junit.Test;

/**
 * @author Andrea Vacondio
 * 
 */
public class IntRangeStringValidatorTest {
    private Validator<String> victim = new IntRangeStringValidator(0, 5);

    @Test
    public void testNegative() {
        Assert.assertFalse(victim.isValid("dsdsa"));
        Assert.assertFalse(victim.isValid("-123"));
    }

    @Test
    public void testPositive() {
        Assert.assertTrue(victim.isValid("0"));
        Assert.assertTrue(victim.isValid("3"));
        Assert.assertTrue(victim.isValid("5"));
    }
}
