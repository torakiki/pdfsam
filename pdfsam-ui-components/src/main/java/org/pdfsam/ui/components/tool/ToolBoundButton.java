/*
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
 * Copyright 2017 by Sober Lemur S.r.l. (info@pdfsam.org).
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
package org.pdfsam.ui.components.tool;

import javafx.scene.control.Button;
import org.apache.commons.lang3.StringUtils;
import org.pdfsam.eventstudio.annotation.EventStation;
import org.pdfsam.model.tool.ToolBound;
import org.pdfsam.ui.components.support.Style;

import static org.apache.commons.lang3.StringUtils.defaultString;

/**
 * {@link Button} owned by a Tool
 *
 * @author Andrea Vacondio
 */
public class ToolBoundButton extends Button implements ToolBound {
    private String toolBinding = StringUtils.EMPTY;

    public ToolBoundButton(String toolBinding) {
        this.toolBinding = defaultString(toolBinding);
        getStyleClass().addAll(Style.BUTTON.css());
    }

    @Override
    @EventStation
    public String toolBinding() {
        return toolBinding;
    }
}
