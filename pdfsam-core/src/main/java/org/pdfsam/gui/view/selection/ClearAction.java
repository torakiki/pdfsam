/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/giu/2013
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
package org.pdfsam.gui.view.selection;

import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.KeyStroke;

import org.bushe.swing.event.EventBus;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.gui.event.EventNamespace;
import org.pdfsam.gui.event.WithEventNamespace;

/**
 * Action to clear the selection table.
 * 
 * @author Andrea Vacondio
 * 
 */
public class ClearAction extends AbstractAction implements WithEventNamespace {

    private EventNamespace namespace;

    public ClearAction(EventNamespace namespace) {
        super(DefaultI18nContext.getInstance().i18n("Clear"));
        this.namespace = namespace;
        this.setEnabled(true);
        this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, InputEvent.ALT_DOWN_MASK));
        this.putValue(Action.SHORT_DESCRIPTION, DefaultI18nContext.getInstance().i18n("Removes every document"));
        this.putValue(Action.SMALL_ICON, new ImageIcon(this.getClass().getResource("/images/clear.png")));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        EventBus.publish(new ClearSelectionTableEvent(namespace));
    }

    public EventNamespace getEventNamespace() {
        return namespace;
    }

}
