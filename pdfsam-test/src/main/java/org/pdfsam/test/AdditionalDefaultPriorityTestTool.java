package org.pdfsam.test;
/*
 * This file is part of the PDF Split And Merge source code
 * Created on 05/04/22
 * Copyright 2022 by Sober Lemur S.r.l. (info@soberlemur.com).
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

import javafx.scene.Node;
import javafx.scene.layout.Pane;
import org.pdfsam.model.tool.Tool;
import org.pdfsam.model.tool.ToolCategory;
import org.pdfsam.model.tool.ToolDescriptor;
import org.pdfsam.model.tool.ToolInputOutputType;
import org.pdfsam.model.tool.ToolPriority;

import static org.pdfsam.model.tool.ToolDescriptorBuilder.builder;

/**
 * @author Andrea Vacondio
 */
public class AdditionalDefaultPriorityTestTool implements Tool {

    public static final String ID = "additional.test.module";

    private final ToolDescriptor descriptor = builder().category(ToolCategory.MERGE).description("Test module")
            .inputTypes(ToolInputOutputType.SINGLE_PDF).name("ATestModule").priority(ToolPriority.DEFAULT)
            .supportURL("http://www.chucknorrisfacts.com/")
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
