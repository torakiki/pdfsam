/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 07/feb/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.gui.support;

import java.util.ArrayList;
import java.util.List;

import static org.apache.commons.lang3.StringUtils.EMPTY;
import static org.apache.commons.lang3.StringUtils.isNotBlank;

import static org.pdfsam.support.RequireUtils.require;

/**
 * Helper class to build multiline tooltips.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ToolTipBuilder {

    private List<String> lines = new ArrayList<>();

    /**
     * Adds the given line to the builder
     * 
     * @param line
     * @return
     */
    public ToolTipBuilder appendLine(String line) {
        require(isNotBlank(line), "ToolTip line cannot be blank");
        lines.add(line);
        return this;
    }

    /**
     * Adds a blank line to the tooltip
     * 
     * @return
     */
    public ToolTipBuilder appendBlankLine() {
        lines.add("");
        return this;
    }

    @Override
    public String toString() {
        if (lines.size() == 0) {
            return EMPTY;
        }
        StringBuilder sb = new StringBuilder("<html><body><div>");
        for (int i = 0; i < lines.size(); i++) {
            if (i != 0) {
                sb.append("<br />");
            }
            sb.append(lines.get(i));
        }
        sb.append("</div></body></html>");
        return sb.toString();
    }

}
