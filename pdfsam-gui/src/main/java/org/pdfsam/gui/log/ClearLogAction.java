/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 15/dic/2011
 * Copyright 2011 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.gui.log;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;

/**
 * Clear text log action.
 * 
 * @author Andrea Vacondio
 * 
 */
class ClearLogAction extends AbstractAction {

    public ClearLogAction() {
        super(DefaultI18nContext.getInstance().i18n("Clear"));
        this.setEnabled(true);
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        EventBus.publish(new ClearLogEvent());
    }

}
