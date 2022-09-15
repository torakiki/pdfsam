/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2013
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
package org.pdfsam.ui.module;

import static org.apache.commons.lang3.StringUtils.defaultString;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.module.ToolBound;
import org.pdfsam.module.Tool;
import org.pdfsam.ui.support.Style;
import org.pdfsam.eventstudio.annotation.EventStation;

import javafx.scene.control.Button;

/**
 * {@link Button} owned by a {@link Tool}
 *
 * @author Andrea Vacondio
 */
public class ToolBoundButton extends Button implements ToolBound {
    private String ownerModule = StringUtils.EMPTY;

    public ToolBoundButton(String ownerModule) {
        this.ownerModule = defaultString(ownerModule);
        getStyleClass().addAll(Style.BUTTON.css());
    }

    @Override
    @EventStation
    public String toolBinding() {
        return ownerModule;
    }
}
