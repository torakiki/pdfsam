/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 4 mag 2019
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
package org.pdfsam.ui.components.selection;

import org.pdfsam.ui.components.tool.ToolBoundButton;
import org.pdfsam.ui.components.support.Style;

/**
 * @author Andrea Vacondio
 */
public class ToolbarButton extends ToolBoundButton {

    public ToolbarButton(String ownerModule) {
        super(ownerModule);
        getStyleClass().addAll(Style.TOOLBAR_BUTTON.css());
    }

}
