/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 24/lug/2014
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
package org.pdfsam.module;

import static org.junit.Assert.assertEquals;
import static org.pdfsam.module.ModuleDescriptorBuilder.builder;

import org.junit.Test;

/**
 * @author Andrea Vacondio
 *
 */
public class ModuleDescriptorBuilderTest {

    @Test(expected = IllegalArgumentException.class)
    public void buildNullCategory() {
        builder().category(null).description("desc").name("name").priority(ModulePriority.DEFAULT).supportURL("url")
                .build();
    }

    @Test(expected = IllegalArgumentException.class)
    public void buildNullName() {
        builder().category(ModuleCategory.MERGE).description("desc").name(null).priority(ModulePriority.DEFAULT)
                .supportURL("url").build();
    }

    @Test(expected = IllegalArgumentException.class)
    public void buildNullDesctription() {
        builder().category(ModuleCategory.MERGE).description(null).name("name").priority(ModulePriority.DEFAULT)
                .supportURL("url").build();
    }

    @Test(expected = IllegalArgumentException.class)
    public void buildBlankName() {
        builder().category(ModuleCategory.MERGE).description("desc").name("").priority(ModulePriority.DEFAULT)
                .supportURL("url").build();
    }

    @Test(expected = IllegalArgumentException.class)
    public void buildBlankDesctription() {
        builder().category(ModuleCategory.MERGE).description("").name("name").priority(ModulePriority.DEFAULT)
                .supportURL("url").build();
    }

    @Test
    public void build() {
        ModuleDescriptor victim = builder().category(ModuleCategory.MERGE).description("desc").name("name")
                .priority(ModulePriority.DEFAULT).supportURL("url").build();
        assertEquals(ModuleCategory.MERGE, victim.category);
        assertEquals("desc", victim.getDescription());
        assertEquals("name", victim.getName());
        assertEquals("url", victim.getSupportURL().get());
        assertEquals(ModulePriority.DEFAULT.getPriority(), victim.getPriority());
    }
}
