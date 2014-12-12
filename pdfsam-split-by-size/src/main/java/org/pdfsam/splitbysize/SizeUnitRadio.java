/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.splitbysize;

import static org.pdfsam.support.RequireUtils.requireNotNull;

import java.util.Map;
import java.util.Optional;

import javafx.scene.control.RadioButton;

import org.pdfsam.ui.workspace.RestorableView;

/**
 * @author Andrea Vacondio
 *
 */
class SizeUnitRadio extends RadioButton implements RestorableView {

    private SizeUnit unit;

    public SizeUnitRadio(SizeUnit unit) {
        requireNotNull(unit, "Unit cannot be null");
        this.unit = unit;
        this.setText(unit.friendlyName());
        this.setId("unit" + unit.symbol());
    }

    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put(unit.toString(), Boolean.TRUE.toString());
        }
    }

    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get(unit.toString())).map(Boolean::valueOf).ifPresent(this::setSelected);
    }

    public SizeUnit unit() {
        return unit;
    }

}
