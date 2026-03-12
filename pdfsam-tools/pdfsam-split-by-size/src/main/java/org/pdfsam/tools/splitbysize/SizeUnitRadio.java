/*
 * This file is part of the PDF Split And Merge source code
 * Created on 12/dic/2014
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
package org.pdfsam.tools.splitbysize;

import javafx.scene.control.RadioButton;
import org.pdfsam.model.ui.workspace.RestorableView;

import java.util.Map;
import java.util.Optional;

import static org.pdfsam.i18n.I18nContext.i18n;
import static org.sejda.commons.util.RequireUtils.requireNotNullArg;

/**
 * Radio button for {@link SizeUnit}
 * 
 * @author Andrea Vacondio
 */
class SizeUnitRadio extends RadioButton implements RestorableView {

    private final SizeUnit unit;

    public SizeUnitRadio(SizeUnit unit) {
        requireNotNullArg(unit, "Unit cannot be null");
        this.unit = unit;
        this.setText(unit.friendlyName());
        this.setId("unit" + unit.symbol());
        this.setAccessibleHelp(i18n().tr("Size unit for the split file size"));
    }

    @Override
    public void saveStateTo(Map<String, String> data) {
        if (isSelected()) {
            data.put(unit.toString(), Boolean.TRUE.toString());
        }
    }

    @Override
    public void restoreStateFrom(Map<String, String> data) {
        Optional.ofNullable(data.get(unit.toString())).map(Boolean::valueOf).ifPresent(this::setSelected);
    }

    public SizeUnit unit() {
        return unit;
    }

}
