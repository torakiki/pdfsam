/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/ago/2014
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
package org.pdfsam.test;

import javafx.scene.Node;
import javafx.scene.layout.Pane;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolCategory;
import org.pdfsam.model.tool.ToolDescriptor;
import org.pdfsam.model.tool.ToolPriority;

import static org.pdfsam.model.tool.ToolDescriptorBuilder.builder;

/**
 * A test module to use in unit tests
 *
 * @author Andrea Vacondio
 */
public class LowPriorityTestTool implements Tool {
    public static final String ID = "low.test.module";

    private final ToolDescriptor descriptor = builder().category(ToolCategory.SPLIT).description("Low priority test module")
            .name("LowPriorityTestModule").priority(ToolPriority.LOW)
            .build();

    @Override
    public String id() {
        return ID;
    }

    @Override
    public ToolDescriptor descriptor() {
        return descriptor;
    }

    @Override
    public Pane panel() {
        return null;
    }

    @Override
    public Node graphic() {
        return null;
    }

}
