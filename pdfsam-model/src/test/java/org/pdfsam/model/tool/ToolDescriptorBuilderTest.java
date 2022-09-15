package org.pdfsam.model.tool;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.pdfsam.model.tool.ToolDescriptorBuilder.builder;

/*
 * This file is part of the PDF Split And Merge source code
 * Created on 15/09/22
 * Copyright 2022 by Sober Lemur S.a.s. di Vacondio Andrea (info@pdfsam.org).
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
class ToolDescriptorBuilderTest {

    @Test
    public void buildNullCategory() {
        assertThrows(NullPointerException.class,
                () -> builder().category(null).description("desc").name("name").priority(ToolPriority.DEFAULT)
                        .supportURL("url")
                        .build());
    }

    @Test
    public void buildNullName() {
        assertThrows(IllegalArgumentException.class,
                () -> builder().category(ToolCategory.MERGE).description("desc").name(null)
                        .priority(ToolPriority.DEFAULT).supportURL("url")
                        .build());
    }

    @Test
    public void buildNullDesctription() {
        assertThrows(IllegalArgumentException.class,
                () -> builder().category(ToolCategory.MERGE).description(null).name("name")
                        .priority(ToolPriority.DEFAULT).supportURL("url")
                        .build());
    }

    @Test
    public void buildBlankName() {
        assertThrows(IllegalArgumentException.class,
                () -> builder().category(ToolCategory.MERGE).description("desc").name("").priority(ToolPriority.DEFAULT)
                        .supportURL("url")
                        .build());
    }

    @Test
    public void buildBlankDesctription() {
        assertThrows(IllegalArgumentException.class,
                () -> builder().category(ToolCategory.MERGE).description("").name("name").priority(ToolPriority.DEFAULT)
                        .supportURL("url")
                        .build());
    }

    @Test
    public void build() {
        ToolDescriptor victim = builder().category(ToolCategory.MERGE).description("desc").name("name")
                .priority(ToolPriority.DEFAULT).supportURL("url")
                .build();
        assertEquals(ToolCategory.MERGE, victim.category());
        assertEquals("desc", victim.description());
        assertEquals("name", victim.name());
        assertEquals("url", victim.supportUrl());
        assertEquals(ToolPriority.DEFAULT.getPriority(), victim.priority());
    }

}